import { Component, inject, input, output } from '@angular/core';
import { Katedra, UserService } from '../../core';
import { Router } from '@angular/router';
import { ButtonComponent } from '../button/button.component';

@Component({
  selector: 'app-department-card',
  standalone: true,
  imports: [ButtonComponent],
  templateUrl: './department-card.component.html',
  styleUrl: './department-card.component.scss',
})
export class DepartmentCardComponent {
  public department = input.required<Katedra>();
  public removable = input<boolean>(false);
  public user = inject(UserService);
  private router = inject(Router);
  public query = output<number>();

  goTo(): void {
    this.router.navigate(['department', this.department().id]);
  }

  delete(value: boolean): void {
    if (value) {
      this.query.emit(this.department().id);
    }
  }
}
