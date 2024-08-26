import { Component, computed, inject } from '@angular/core';
import { Korisnik, UserService } from '../../core';
import { CourseCardComponent } from '../../shared/course-card/course-card.component';
import { openModal } from '../../shared';
import { Dialog } from '@angular/cdk/dialog';

@Component({
  selector: 'app-profile',
  standalone: true,
  imports: [CourseCardComponent],
  templateUrl: './profile.component.html',
  styleUrl: './profile.component.scss',
})
export class ProfileComponent {
  public user = inject(UserService);
  public courses = computed(() => {
    return this.user.user().kolegiji;
  });

  private dialog = inject(Dialog);

  openEdit(): void {
    let ref = openModal<Korisnik>(this.dialog, {
      data: structuredClone(this.user.user()),
      tool: { view: ProfileComponent },
      inputs: {},
      title: 'Edit section',
    });
    ref.componentInstance['query'].subscribe((user: Korisnik) => {
      if (user) {
        console.log(user);
        // TODO: UREĐIVANJE SEKCIJE QUERY
      }
      ref.close();
    });
  }
}
