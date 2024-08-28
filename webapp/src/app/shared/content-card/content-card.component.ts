import {
  Component,
  computed,
  inject,
  input,
  TemplateRef,
  viewChild,
} from '@angular/core';
import { Sadrzaj, UserService } from '../../core';
import { CommonModule, DatePipe } from '@angular/common';
import { RouterModule } from '@angular/router';

@Component({
  selector: 'app-content-card',
  standalone: true,
  imports: [DatePipe, CommonModule, RouterModule],
  templateUrl: './content-card.component.html',
  styleUrl: './content-card.component.scss',
})
export class ContentCardComponent {
  public sadrzaj = input.required<Sadrzaj>();
  private lekcija = viewChild('lekcija', { read: TemplateRef });
  private poveznica = viewChild('poveznica', { read: TemplateRef });
  private dokument = viewChild('dokument', { read: TemplateRef });
  private kviz = viewChild('kviz', { read: TemplateRef });
  public user = inject(UserService);

  public datum = computed(() => {
    let date = new Date(
      parseInt(this.sadrzaj().vrijednost.vrijeme_kreiranja.toString()) * 10000,
    );
    return `${date.toLocaleDateString()} ${date.toLocaleTimeString()}`;
  });

  public outlet(): TemplateRef<unknown> {
    switch (this.sadrzaj()?.tip) {
      case 'lekcija':
        return this.lekcija();
      case 'poveznica':
        return this.poveznica();
      case 'dokument':
        return this.dokument();
      default:
        return this.kviz();
    }
  }
}
