import { Component, inject, input, output } from '@angular/core';
import { Dialog } from '@angular/cdk/dialog';
import { openConfirmation } from '../confirmation/confirmation.component';

@Component({
  selector: 'app-button',
  standalone: true,
  imports: [],
  templateUrl: './button.component.html',
  styleUrl: './button.component.scss',
})
export class ButtonComponent {
  private dialog = inject(Dialog);
  public title = input.required<string>();
  public query = output<boolean>();
  openEditSection(): void {
    let ref = openConfirmation(this.dialog, {
      title: `Delete ${this.title()}`,
    });
    ref.componentInstance['query'].subscribe((res: boolean) => {
      if (res) {
        this.query.emit(res);
      }
      ref.close();
    });
  }
}
